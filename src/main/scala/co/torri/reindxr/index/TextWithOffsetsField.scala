package co.torri.reindxr.index

import co.torri.reindxr.index.TextWithOffsetsField.fieldType
import org.apache.lucene.document.{Field, FieldType}


class TextWithOffsetsField(name: String, text: String) extends Field(name, text, fieldType)

object TextWithOffsetsField {

  private def fieldType: FieldType = {
    val ft = new FieldType()
    ft.setStoreTermVectors(true)
    ft.setStoreTermVectorPositions(true)
    ft.setStoreTermVectorOffsets(true)
    ft.setIndexed(true)
    ft.setTokenized(true)
    ft
  }

}
