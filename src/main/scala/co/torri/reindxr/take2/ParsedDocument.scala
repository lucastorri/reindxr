package co.torri.reindxr.take2

case class ParsedDocument(
                           documentId: DocumentId,
                           language: String,
                           content: String,
                           contentMetadata: Map[String, String],
                           userMetadata: Map[String, String],
                         )
