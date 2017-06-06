package MySqlPaser.MysqlAst.CreateDefination

import MySqlPaser.Helper.Helper
/**
  * Created by npvinh on 6/5/17.
  */
case class UniqueKeyDefination(key:String, list:List[String]) extends CreateDefination {
  override def toString: String = {
    val result = "$table->unique([" + list.map(a => Helper.escapeStringSql(a)).mkString(", ") +"])"
    result
  }
}