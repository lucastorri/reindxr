package co.torri.reindxr.take2

import cats.effect.IO
import org.apache.tika.detect.DefaultDetector
import org.apache.tika.language.LanguageIdentifier
import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.AutoDetectParser
import org.apache.tika.sax.BodyContentHandler

class TikaDocumentParser extends DocumentParser {
  private val parser = new AutoDetectParser(new DefaultDetector)

  override def parse(document: Document): IO[ParsedDocument] =
    document.content.map { in =>
      val contentHandler = new BodyContentHandler(Int.MaxValue)
      val metadata = new Metadata
      parser.parse(in, contentHandler, metadata)
      val content = contentHandler.toString
      val language = new LanguageIdentifier(content).getLanguage

      ParsedDocument(
        document.id,
        language,
        content,
        metadata.names().map(name => name -> metadata.get(name)).toMap,
        document.metadata,
      )
    }
}
