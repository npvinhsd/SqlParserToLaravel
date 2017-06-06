package MysqlParser

import java.io.PrintWriter

import MySqlPaser.MysqlAst
import MySqlPaser.MysqlAst.CreateDefination._
import MySqlPaser.MysqlAst.ColumnOption._
import MySqlPaser.MysqlAst.Stmt._
import MySqlPaser.MysqlAst.DataType._

import scala.io.Source
import scala.util.parsing.combinator._

/**
  * Created by npvinh on 6/1/17.
  */
object MysqlParser extends RegexParsers{
  def query = (commentStmt | dropTableStmt| createTableStmt)*
  def commentStmt = lineComment | blockComment
  def lineComment = """--.*?\n""".r ^^ {_ => CommentStmt()}
  def blockComment = """/\*.*?\*/;""".r ^^ {_ => CommentStmt()}

  def createTableStmt = """CREATE TABLE""".r ~> stringLiteral ~ createDefinitions <~ """.*?;""".r ^^ {
    case tableName ~ listDefination => CreateTableStmt(tableName, listDefination.asInstanceOf[List[CreateDefination]])
  }

  def createDefinitions = "(\n" ~> repsep(createDefinition, ",") <~ ")"
  def createDefinition = tableDefintions | keyDefinition | primaryKeyDefinition| constantDefinition | uniqueKeyDefinition
  def tableDefintions = stringLiteral ~ columnOption ^^ {case columnName ~ columnOption => {
    TableDefination(columnName, columnOption)
  }}
  def keyDefinition = "KEY" ~> stringLiteral ~ ( "(" ~> repsep(stringLiteral, ",") <~ ")") ^^ {
    case key ~ list => KeyDefination(key, list)
  }
  def uniqueKeyDefinition = "UNIQUE KEY" ~> stringLiteral ~ ( "(" ~> repsep(stringLiteral, ",") <~ ")") ^^ {
    case key ~ list => UniqueKeyDefination(key, list)
  }
  def primaryKeyDefinition = "PRIMARY KEY" ~ "(" ~> repsep(stringLiteral,",") <~ ")" ^^ {
    case key => PrimaryKeyDefination(key)
  }

  def constantDefinition = "CONSTRAINT" ~> stringLiteral ~ ("FOREIGN KEY" ~> ("(" ~> stringLiteral <~ ")"))  ~ reference ^^ {
    case column ~ foreignKey ~ (tableReference ~ columnReference) => ConstrainDefination(column, foreignKey, tableReference, columnReference)
  }
  def reference = "REFERENCES" ~> stringLiteral ~ ("(" ~> stringLiteral <~")") <~ on
  def on = "ON DELETE CASCADE" | "ON DELETE SET NULL"

  def columnOption = dataType ~ rep(option) ^^ {
    case dataType ~ option =>
      ColumnDefination(dataType.asInstanceOf[DataTypeSql],option.asInstanceOf[List[Option]])
  }

  def option = nullable | default | autoIncrement | onUpdate

  def nullable = canNull | notNull
  def canNull = "NULL" ^^ {_ => true} ^^ {_ => Nullable(true)}
  def notNull = "NOT NULL" ^^ {_ => false} ^^ {_ => Nullable(false)}
  def onUpdate = "ON UPDATE CURRENT_TIMESTAMP" ^^ {
    _ => OnUpdate()
  }

  def autoIncrement = "AUTO_INCREMENT" ^^ {
    _ => AutoIncrement()
  }

  def default = "DEFAULT" ~> value ^^ {
    value => Default(value)
  }
  def value = "NULL" | valueInQuote | currentTimeStamp
  def currentTimeStamp = "CURRENT_TIMESTAMP"
  def valueInQuote = "'" ~> """[\d\w_\-.]*""".r <~ "'".r

  def dataType = int | timestamp | datetime | date  | char | double | tinyint | text | longtext | varchar | bigint
  def int = "int" ~> length.? ~ "unsigned".? ^^ {
    case Some(length) ~ Some(unsigned) => Int(length.toInt,true)
    case Some(length) ~ None => Int(length.toInt, false)
    case None ~ Some(unsigned) => Int(-1, true)
    case None ~ None => Int(-1, false)
  }
  def timestamp = "timestamp" ^^ {_ => Timestamp()}
  def date = "date" ^^ {_ => Date()}
  def datetime = "datetime" ^^ {_ => DateTime()}
  def char = "char" ~> length.? ~ collate ^^ {
    case Some(length) ~ collate => Char(length.toInt)
    case None ~ collate => Char(-1)
  }
  def double = "double" ~> lengthDouble.? ~ "unsigned".? ^^ {
    case Some(a ~ b) ~ Some(unsigned) => Double(a.toInt,b.toInt,true)
    case Some(a ~ b) ~ None => Double(a.toInt,b.toInt, false)
    case None ~ Some(unsigned) => Double(-1, -1, true)
    case None ~ None => Double(-1, -1, false)
  }

  def tinyint = "tinyint" ~> length.? ~ "unsigned".? ^^ {
    case Some(length) ~ Some(unsigned) => TinyInt(length.toInt,true)
    case Some(length) ~ None => TinyInt(length.toInt, false)
    case None ~ Some(unsigned) => TinyInt(-1, true)
    case None ~ None => TinyInt(-1, false)
  }

  def bigint = "bigint" ~> length.? ~ "unsigned".? ^^ {
    case Some(length) ~ Some(unsigned) => BigInt(length.toInt,true)
    case Some(length) ~ None => BigInt(length.toInt, false)
    case None ~ Some(unsigned) => BigInt(-1, true)
    case None ~ None => BigInt(-1, false)
  }
  def text = "text" ~> collate ^^ {
    case collate => Text()
  }

  def longtext = "longtext" ~> collate ^^ {
    case collate => LongText()
  }
  def varchar = "varchar" ~> length.? ~ collate ^^ {
    case Some(length) ~ collate => Varchar(length.toInt)
    case None ~ collate => Varchar(-1)
  }
  def length = "(" ~> """\d*""".r <~ ")"
  def collate = """COLLATE""" ~> """[\w\d_]*""".r
  def lengthDouble =  "(" ~> """\d*""".r ~ ("," ~> """\d*""".r <~ ")")

  def dropTableStmt = """DROP TABLE.*?;""".r ^^ {_ => DropTableStmt()}

  def stringLiteral = "`" ~> """[^`]*""".r <~ "`"

  def main(args: Array[String]): Unit = {
    val filePath = "/Users/npvinh/Workspace/Scala/SqlParser/src/main/scala/MySqlPaser/mysql"
    val fileContent = Source.fromFile(filePath).mkString

    parse(query, fileContent) match {
      case Success(stmts, _) => {
        stmts.foreach(create => create match {
          case a: CommentStmt => {

          }
          case b: CreateTableStmt => {
            new PrintWriter("./2017_06_06_000000_create_" + b.tableName.substring(3) + "_table.php") {
              write(b.toString)
              close
            }
          }
          case c: DropTableStmt => {

          }
        })
      }
      case Failure(msg, _) => println("Failure: " + msg)
      case Error(msg, _) => println("Error: " + msg)
    }
  }
}
