module App

open Elmish
open Elmish.React
open Feliz
open Thoth.Json
open Fable.SimpleHttp

type HackernewsItem = {
  id: int
  title: string
  url: string option
}

type State = {
  StoryItems: Deferred<Result<HackernewsItem list, string>>
}

type Msg =
  | LoadStoryItems of AsyncOperationEvent<Result<HackernewsItem list, string>>

let init() =
  let initialState = { StoryItems = HasNotStartedYet }
  let initialCmd = Cmd.ofMsg (LoadStoryItems Started)
  initialState, initialCmd

let itemDecoder : Decoder<HackernewsItem> =
  Decode.object (fun fields -> {
    id = fields.Required.At [ "id" ] Decode.int
    title = fields.Required.At [ "title" ] Decode.string
    url = fields.Optional.At [ "url" ] Decode.string
  })

let storiesEndpoint = "https://hacker-news.firebaseio.com/v0/topstories.json"

let loadStoryItem (itemId: int) = async {
  let endpoint = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" itemId
  let! (status, responseText) = Http.get endpoint
  match status with
  | 200 ->
    match Decode.fromString itemDecoder responseText with
    | Ok storyItem -> return Some storyItem
    | Error _ -> return None
  | _ ->
    return None
}

let loadStoryItems = async {
  let! (status, responseText) = Http.get storiesEndpoint
  match status with
  | 200 ->
    // parse the response text as a list of IDs (integers)
    let storyIds = Decode.fromString (Decode.list Decode.int) responseText
    match storyIds with
    | Ok storyIds ->
        // take the first 10 IDs
        // load the item from each ID in parallel
        // aggregate the results into a single list
        let! storyItems =
          storyIds
          |> List.truncate 10
          |> List.map loadStoryItem
          |> Async.Parallel
          |> Async.map (Array.choose id >> List.ofArray)

        return LoadStoryItems (Finished (Ok storyItems))

    | Error errorMsg ->
        // could not parse the array of story ID's
        return LoadStoryItems (Finished (Error errorMsg))
  | _ ->
      // non-OK response goes finishes with an error
      return LoadStoryItems (Finished (Error responseText))
}

let update (msg: Msg) (state: State) =
  match msg with
  | LoadStoryItems Started ->
      let nextState = { state with StoryItems = InProgress }
      nextState, Cmd.fromAsync loadStoryItems

  | LoadStoryItems (Finished (Ok storyItems)) ->
      let nextState = { state with StoryItems = Resolved (Ok storyItems) }
      nextState, Cmd.none

  | LoadStoryItems (Finished (Error error)) ->
      let nextState = { state with StoryItems = Resolved (Error error) }
      nextState, Cmd.none

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]

let renderItem item =
  Html.div [
    prop.key item.id
    prop.className "box"
    prop.style [ style.marginTop 15; style.marginBottom 15 ]
    prop.children [
      match item.url with
      | Some url ->
          Html.a [
            prop.style [ style.textDecoration.underline ]
            prop.target.blank
            prop.href url
            prop.text item.title
          ]
      | None ->
          Html.p item.title
    ]
  ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]

let renderItems = function
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> renderError errorMsg
  | Resolved (Ok items) -> React.fragment [ for item in items -> renderItem item ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "Elmish Hackernews"
      ]

      renderItems state.StoryItems
    ]
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run