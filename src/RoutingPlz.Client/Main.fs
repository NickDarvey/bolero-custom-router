module RoutingPlz.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Json
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

/// Routing endpoints definition.
type Page =
    | (*[<EndPoint "/">]       *) Home
    | (*[<EndPoint "/counter">]*) Counter

/// The Elmish application's model.
type Model =
    {
        page: Page
        counter: int
        books: Book[] option
        error: string option
        username: string
        password: string
        signedInAs: option<string>
        signInFailed: bool
    }

and Book =
    {
        title: string
        author: string
        [<DateTimeFormat "yyyy-MM-dd">]
        publishDate: DateTime
        isbn: string
    }

let initModel =
    {
        page = Home
        counter = 0
        books = None
        error = None
        username = ""
        password = ""
        signedInAs = None
        signInFailed = false
    }

/// Remote service definition.
type BookService =
    {
        /// Get the list of all books in the collection.
        getBooks: unit -> Async<Book[]>

        /// Add a book in the collection.
        addBook: Book -> Async<unit>

        /// Remove a book from the collection, identified by its ISBN.
        removeBookByIsbn: string -> Async<unit>

        /// Sign into the application.
        signIn : string * string -> Async<option<string>>

        /// Get the user's name, or None if they are not authenticated.
        getUsername : unit -> Async<string>

        /// Sign out from the application.
        signOut : unit -> Async<unit>
    }

    interface IRemoteService with
        member this.BasePath = "/books"

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | Increment
    | Decrement
    | SetCounter of int

let update message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none

    | Increment ->
        { model with counter = model.counter + 1 }, Cmd.none
    | Decrement ->
        { model with counter = model.counter - 1 }, Cmd.none
    | SetCounter value ->
        { model with counter = value }, Cmd.none

/// Connects the routing system to the Elmish application.
//let router = Router.infer SetPage (fun model -> model.page)

let router : Router<Page, Model, Message> = {
  getEndPoint = fun m -> m.page
  setRoute = fun path ->
      match path.Trim('/').Split('/') with
      | [||]     -> Some <| Home
      | [|"c" |] -> Some <| Counter
      //| [|"d"; wid;"check-in"|] -> Some <| Data
      | _ -> None
      |> Option.map SetPage
  getRoute = function
      | Page.Home    -> "/"
      | Page.Counter -> "/c"
      //| Page.Data    -> "/d"
}

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main.Home().Elt()

let counterPage model dispatch =
    Main.Counter()
        .Decrement(fun _ -> dispatch Decrement)
        .Increment(fun _ -> dispatch Increment)
        .Value(model.counter, fun v -> dispatch (SetCounter v))
        .Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat [
            menuItem model Home "Home"
            menuItem model Counter "Counter"
        ])
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
            | Counter -> counterPage model dispatch
        )
        .Error(empty)
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
//#if DEBUG
//        |> Program.withHotReload
//#endif
