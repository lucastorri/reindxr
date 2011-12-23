package co.torri.reindxr.client

import co.torri.reindxr.index._
import akka.actor.Actor
import Actor._
import akka.event.EventHandler


object Client {

	def run() {
	    val actor = remote.actorFor("search-service", "localhost", 8123)
		while (true) {
			println("search> ")
		    val result = (actor !! SearchIndex(readLine)).as[SearchIndexResult]
			println(result)
		}
	}
	
	def main(args: Array[String]) {
		run
	}

}