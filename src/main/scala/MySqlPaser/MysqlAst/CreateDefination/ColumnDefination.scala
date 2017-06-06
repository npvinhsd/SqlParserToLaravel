package MySqlPaser.MysqlAst.CreateDefination

import MySqlPaser.MysqlAst.ColumnOption.Option
import MySqlPaser.MysqlAst.DataType.DataTypeSql

/**
  * Created by npvinh on 6/5/17.
  */
case class ColumnDefination(dataType: DataTypeSql,
                            options:List[Option])
