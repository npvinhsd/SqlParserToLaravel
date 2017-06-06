package MySqlPaser.MysqlAst.Stmt

import MySqlPaser.MysqlAst.CreateDefination._
import MySqlPaser.Helper.Helper
/**
  * Created by npvinh on 6/5/17.
  */
case class CreateTableStmt(tableName:String, createDefination:List[CreateDefination]) extends Stmt {
  override def toString: String = {
    var result = "<?php\n"
    result += "\n"
    result += "use Illuminate\\Database\\Schema\\Blueprint;\n"
    result += "use Illuminate\\Database\\Migrations\\Migration;\n"
    result += "\n"
    result += "class " + "Create" +Helper.camelTableName(tableName) + "Table extends Migration {\n"
    result += "\t/**\n"
    result += "\t* Run the migrations.\n"
    result += "\t*\n"
    result += "\t* @return void\n"
    result += "\t*/\n"
    result += "\tpublic function up()\n"
    result += "\t{\n"
    result += "\t\tSchema::create(" + Helper.escapeTableNameSql(tableName) +", function (Blueprint $table) {\n"
    result += "\t\t\t" + createDefination.filter(a => !a.toString.isEmpty).mkString(";\n\t\t\t") + ";\n"
    result += "\t\t});\n"
    result += "\t}\n"
    result += "\t\n"
    result += "\t/**\n"
    result += "\t* Reverse the migrations.\n"
    result += "\t*\n"
    result += "\t* @return void\n"
    result += "\t*/\n"
    result += "\tpublic function down()\n"
    result += "\t{\n"
    result += "\t\tSchema::drop(" + Helper.escapeTableNameSql(tableName) +");\n"
    result += "\t}\n"
    result += "}\n"
    result
  }
}
