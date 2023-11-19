package ch.epfl.bluebrain.nexus

import java.nio.file.{Path => JavaPath, Paths}
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Path
import akka.stream.alpakka.file.scaladsl.Directory
import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import cats.effect.{IO, LiftIO}
import ch.epfl.bluebrain.nexus.storage.File.FileAttributes
import ch.epfl.bluebrain.nexus.storage.config.AppConfig.StorageConfig
import ch.epfl.bluebrain.nexus.storage.config.Contexts.errorCtxIri
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

import java.net.URLDecoder
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

package object storage {

  /**
    * Source where the Output is ByteString and the Materialization is Any
    */
  type AkkaSource = Source[ByteString, Any]

  /**
    * Rejection or file attributes
    */
  type RejOrAttributes = Either[Rejection, FileAttributes]

  /**
    * Rejection or Path wrapped
    */
  type RejOrPath = Either[Rejection, JavaPath]

  /**
    * Rejection or Out attributes
    */
  type RejOr[Out] = Either[Rejection, Out]

  implicit val encUriPath: Encoder[Path] = Encoder.encodeString.contramap(_.toString())
  implicit val decUriPath: Decoder[Path] = Decoder.decodeString.emapTry(s => Try(Path(s)))

  implicit class FutureSyntax[A](private val future: Future[A]) extends AnyVal {
    def to[F[_]](implicit F: LiftIO[F], ec: ExecutionContext): F[A] = {
      implicit val contextShift = IO.contextShift(ec)
      F.liftIO(IO.fromFuture(IO(future)))
    }
  }

  implicit class PathSyntax(private val path: JavaPath) extends AnyVal {

    /**
      * Converts a Java Path to an Akka [[Uri]]
      */
    def toAkkaUri: Uri = {
      val pathString = path.toUri.toString
      if (pathString.endsWith("/")) Uri(pathString.dropRight(1)) else Uri(pathString)
    }
  }

  /**
    * Build a Json error message that contains the keys @context and @type
    */
  def jsonError(json: Json): Json = {
    val typed = json.hcursor.get[String]("@type").map(v => Json.obj("@type" -> v.asJson)).getOrElse(Json.obj())
    typed deepMerge Json.obj("@context" -> Json.fromString(errorCtxIri.toString))
  }

  def folderSource(path: JavaPath): AkkaSource = Directory.walk(path).via(TarFlow.writer(path))

  def fileSource(path: JavaPath): AkkaSource = FileIO.fromPath(path)

  /**
    * Checks if the ''target'' path is a descendant of the ''parent'' path. E.g.: path = /some/my/path ; parent = /some
    * will return true E.g.: path = /some/my/path ; parent = /other will return false
    */
  def descendantOf(target: JavaPath, parent: JavaPath): Boolean =
    inner(parent, target.getParent)

  @tailrec
  @SuppressWarnings(Array("NullParameter"))
  def inner(parent: JavaPath, child: JavaPath): Boolean = {
    if (child == null) false
    else if (parent == child) true
    else inner(parent, child.getParent)
  }

  private def decode(path: Uri.Path): String =
    Try(URLDecoder.decode(path.toString, "UTF-8")).getOrElse(path.toString())

  def basePath(config: StorageConfig, name: String, protectedDir: Boolean = true): JavaPath = {
    val path = config.rootVolume.resolve(name).normalize()
    if (protectedDir) path.resolve(config.protectedDirectory).normalize() else path
  }

  def filePath(config: StorageConfig, name: String, path: Uri.Path, protectedDir: Boolean = true): JavaPath = {
    val filePath = Paths.get(decode(path))
    if (filePath.isAbsolute) filePath.normalize()
    else basePath(config, name, protectedDir).resolve(filePath).normalize()
  }
}
