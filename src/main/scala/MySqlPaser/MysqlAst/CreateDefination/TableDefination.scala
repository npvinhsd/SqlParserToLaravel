package MySqlPaser.MysqlAst.CreateDefination
import MySqlPaser.MysqlAst.DataType._
import MySqlPaser.MysqlAst.ColumnOption._
import MySqlPaser.Helper.Helper
/**
  * Created by npvinh on 6/5/17.
  */
case class TableDefination(columnName:String, columnDefination: ColumnDefination) extends CreateDefination {
  override def toString: String = {
    var result = "$table->"
    columnDefination.dataType match {
      case a: BigInt => {
        result = result + "bigInteger(" + Helper.escapeStringSql(columnName) + ")"
        if(a.unsigned)
          result = result + "->unsigned()"
      }
      case b: Char => result = result + "char(" + Helper.escapeStringSql(columnName) + "," + b.length + ")"
      case c: Date => result = result + "date(" + Helper.escapeStringSql(columnName) + ")"
      case d: DateTime => result = result + "dateTime(" + Helper.escapeStringSql(columnName) + ")"
      case e: Double => result = result + "double(" + Helper.escapeStringSql(columnName) + ", " + e.length + ", " + e.decimals + ")"
      case f: Int => {
        result = result + "integer(" + Helper.escapeStringSql(columnName) + ")"
        if(f.unsigned)
          result = result + "->unsigned()"
      }
      case g: LongText => result = result + "longText(" + Helper.escapeStringSql(columnName) + ")"
      case h: Text => result = result + "text(" + Helper.escapeStringSql(columnName) + ")"
      case i: Timestamp => result += "timestamp(" + Helper.escapeStringSql(columnName) + ")"
      case j: TinyInt => {
        result = result + "tinyInteger(" + Helper.escapeStringSql(columnName) + ")"
        if(j.unsigned)
          result = result + "->unsigned()"
      }
      case k: Varchar => result = result + "string(" + Helper.escapeStringSql(columnName) + ", " + k.length + ")"
    }

    columnDefination.options.foreach(e => e match {
      case a:AutoIncrement => columnDefination.dataType match  {
        case aa: Int => return "$table->increments(" + Helper.escapeStringSql(columnName) + ")"
        case ab: BigInt => return "$table->bigIncrements(" + Helper.escapeStringSql(columnName) + ")"
        case ac: TinyInt => return "$table->tinyInteger(" + Helper.escapeStringSql(columnName) + ")"
      }
      case b: Default => result = result + "->default(" + b.value + ")"
      case c: Nullable => {
        if (c.value) {
          result += "->nullable()"
        }
      }
      case d: OnUpdate => {
      }
    })
    return result
  }
}
