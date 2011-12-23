package co.torri.reindxr

import co.torri.reindxr.index._
import akka.actor.Actor
import Actor._
import akka.event.EventHandler
import akka.actor.ActorRef


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