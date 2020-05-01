package co.torri.reindxr.index

import org.apache.lucene.document.{Field, FieldType}


class TextWithOffsetsField(name: String, text: String) extends Field(name, text, TextWithOffsetsField.ft)

object TextWithOffsetsField {

  private def ft: FieldType = {
    val ft = new FieldType()
    ft.setStoreTermVectors(true)
    ft.setStoreTermVectorPositions(true)
    ft.setStoreTermVectorOffsets(true)
    ft.setIndexed(true)
    ft.setTokenized(true)
    ft
  }

}
