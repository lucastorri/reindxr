package co.torri.reindxr.take2

import cats.effect.IO

trait DocumentParser {
  def parse(document: Document): IO[ParsedDocument]
}
