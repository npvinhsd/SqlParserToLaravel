package MySqlPaser.Helper

/**
  * Created by npvinh on 6/5/17.
  */
object Helper {
  def escapeStringSql(value: String): String = {
    //TODO: Need escape
    var result = "'" + value + "'"
    result
  }

  def escapeTableNameSql(value: String): String = {
    var result = value.substring(3)
    result = escapeStringSql(result)
    result
  }

  def camelString(value: String): String = {
    val split = value.split("_")
    split.filter(a => !a.isEmpty).map(a => a.head.toUpper + a.substring(1)).mkString("")
  }

  def camelTableName(value: String): String = {
    var result = value.substring(3)
    result = camelString(result)
    result
  }
}
