package co.torri.reindxr.index

import co.torri.reindxr.index.TextWithOffsetsField.fieldType
import org.apache.lucene.document.{Field, FieldType}
import org.apache.lucene.index.IndexOptions


class TextWithOffsetsField(name: String, text: String) extends Field(name, text, fieldType)

object TextWithOffsetsField {

  private def fieldType: FieldType = {
    val ft = new FieldType()
    ft.setStoreTermVectors(true)
    ft.setStoreTermVectorPositions(true)
    ft.setStoreTermVectorOffsets(true)
    ft.setIndexOptions(IndexOptions.DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS)
    ft.setTokenized(true)
    ft
  }

}
