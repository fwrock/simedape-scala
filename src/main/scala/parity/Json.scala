package org.interscity.htc.parity

import spray.json.*
import java.nio.file.{Files, Path, StandardCopyOption, AtomicMoveNotSupportedException}
import java.nio.charset.StandardCharsets.UTF_8

object Json:
  val empty: JsObject = JsObject()
  def obj(fields: (String, JsValue)*): JsObject = JsObject(fields.toMap)
  def num(value: Double): JsValue = if value.isFinite then JsNumber(value) else JsNull
  def arr(values: Iterable[Double]): JsArray = JsArray(values.iterator.map(num).toVector)
  def ints(values: Iterable[Int]): JsArray = JsArray(values.iterator.map(v => JsNumber(v)).toVector)
  def strings(values: Iterable[String]): JsArray = JsArray(values.iterator.map(JsString(_)).toVector)
  def read(path: Path): JsValue = Files.readString(path, UTF_8).parseJson
  def write(path: Path, value: JsValue): Unit =
    Files.createDirectories(path.toAbsolutePath.getParent)
    val temporary = Files.createTempFile(path.toAbsolutePath.getParent, ".write-", ".tmp")
    try
      Files.writeString(temporary, value.prettyPrint, UTF_8)
      try Files.move(temporary, path.toAbsolutePath, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      catch case _: AtomicMoveNotSupportedException => Files.move(temporary, path.toAbsolutePath, StandardCopyOption.REPLACE_EXISTING)
    finally Files.deleteIfExists(temporary)
  def safe(root: Path, name: String): Path =
    require(name.nonEmpty && !name.contains('\\') && !Path.of(name).isAbsolute, "Invalid relative path")
    val base = root.toAbsolutePath.normalize()
    val result = base.resolve(name).normalize()
    require(result.startsWith(base) && result != base, "Path escapes its root")
    val existing = Iterator.iterate(result)(_.getParent).takeWhile(_ != null).find(Files.exists(_)).get
    if Files.exists(base) then require(existing.toRealPath().startsWith(base.toRealPath()), "Symbolic link escapes root")
    result
  extension (value: JsValue)
    def field(name: String, default: JsValue = JsNull): JsValue = value match
      case o: JsObject => o.fields.getOrElse(name, default)
      case _ => default
    def text(default: String = ""): String = value match
      case JsString(s) => s
      case JsNumber(n) => n.toString
      case JsNull => default
      case _ => throw IllegalArgumentException("Expected a string")
    def number(default: Double = 0.0): Double = value match
      case JsNumber(n) => n.toDouble
      case JsString(s) => s.toDouble
      case JsNull => default
      case _ => throw IllegalArgumentException("Expected a number")
    def int(default: Int = 0): Int =
      val result = number(default.toDouble)
      require(result.isValidInt, "Expected an integer")
      result.toInt
    def bool(default: Boolean = false): Boolean = value match
      case JsBoolean(v) => v
      case JsNull => default
      case _ => throw IllegalArgumentException("Expected a boolean")
    def values: Vector[JsValue] = value match
      case JsArray(vs) => vs
      case JsNull => Vector.empty
      case _ => throw IllegalArgumentException("Expected an array")
    def names: Vector[String] = values.map(_.text())
