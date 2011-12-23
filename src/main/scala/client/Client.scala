package co.torri.reindxr

import akka.actor.Actor.remote
import akka.actor.Actor.toAnyOptionAsTypedOption
import akka.actor.actorRef2Scala
import akka.actor.ActorRef
import co.torri.reindxr.index.SearchIndex
import co.torri.reindxr.index.SearchIndexResult


package object client {

    class ReindxrClient(private val client: ActorRef) {

        def search(query: String) = 
            (client !! SearchIndex(readLine)).as[SearchIndexResult]
            
    } 
    
    def connect(server: String, port: Int) =
        new ReindxrClient(remote.actorFor("search-service", server, port))

    def onServer[T](server: String, port: Int)(exec: ReindxrClient => T) : T =
        Some(connect(server, port)).map(client => exec(client)).get

}