
//(client ? SearchIndex(readLine)).as[SearchIndexResult]

//def connectTo(server: String, port: Int) =
//new ReindxrClient(remote.actorFor("search-service", server, port))

//def onServer[T](server: String, port: Int)(exec: ReindxrClient => T) : T =
//  Some(connect(server, port)).map(client => exec(client)).get
