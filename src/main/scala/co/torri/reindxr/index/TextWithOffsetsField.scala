package co.torri.reindxr.index

import org.apache.lucene.document.{FieldType, Field}


class TextWithOffsetsField(name: String, text: String) extends Field(name, text, TextWithOffsetsField.ft)
object TextWithOffsetsField {

  private[TextWithOffsetsField] def ft = {
    val ft = new FieldType()
    ft.setStoreTermVectors(true)
    ft.setStoreTermVectorPositions(true)
    ft.setStoreTermVectorOffsets(true)
    ft.setIndexed(true)
    ft.setTokenized(true)
    ft
  }

}
