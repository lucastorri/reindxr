package co.torri.reindxr.take2

import java.nio.file.{Files, Path, Paths}

object Main {
  def main(args: Array[String]): Unit = {
    val documentsDirectory: Path = Paths.get("/Users/lucastorri/tmp/docs")
    val indexDirectory: Path = Paths.get("/Users/lucastorri/tmp/index")
    val parser: DocumentParser = new TikaDocumentParser

    def store(accountId: AccountId): DocumentStore =
      new FilesystemDocumentStore(Files.createDirectories(documentsDirectory.resolve(accountId.value)))

    def index(accountId: AccountId): DocumentIndex =
      new LuceneDocumentIndex(Files.createDirectories(indexDirectory.resolve(accountId.value)), parser, store(accountId))

    def reindxr(accountId: AccountId): Reindxr =
      new ExternalIndexReindxr(store(accountId), index(accountId))

    val default = reindxr(AccountId("default"))

    //    default.add(new Document {
    //      override def id: DocumentId = DocumentId("hi!")
    //
    //      override def content: IO[InputStream] = IO(Files.newInputStream(Paths.get("/Users/lucastorri/Downloads/WHO-COVID-19-Community_Transmission-2020.1-eng.pdf")))
    //
    //      override def metadata: Map[String, String] = Map(
    //        "id" -> "hi!",
    //        "length" -> "big",
    //        "timestamp" -> "long ago",
    //      )
    //    }).unsafeRunSync()

    println("===============")
    default.search("WHO").unsafeRunSync().foreach(println)
    //    default.search("length:big WHO").unsafeRunSync().foreach(println)
    println("---------------")
    default.snippets("outbreak").unsafeRunSync().foreach(println)
    default.snippets(DocumentId("hi!"), "outbreak").unsafeRunSync().foreach(println)
    default.highlight(DocumentId("hi!"), "outbreak").unsafeRunSync().foreach(println)
  }
}
