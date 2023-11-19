package ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations

import akka.http.scaladsl.model.Uri
import cats.effect.{Blocker, ContextShift, IO}
import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.model.FileAttributes.FileAttributesOrigin.Client
import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.model.{FileAttributes, FileDescription}
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.model.Storage.DiskStorage
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.disk.DiskStorageSaveFile.initLocation
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.remote.RemoteDiskStorageLinkFile

import java.nio.file.{CopyOption, Path, StandardCopyOption}
import scala.concurrent.ExecutionContext

object CopyFile {

  def copyLocal(destDesc: FileDescription, destStorage: DiskStorage, sourceAttr: FileAttributes): IO[FileAttributes] = {
    val ec                            = ExecutionContext.global
    implicit val cs: ContextShift[IO] = IO.contextShift(ec)
    val blocker: Blocker              = Blocker.liftExecutionContext(ec)
    val source: Path                  = Path.of("/")
    val dest: Path                    = Path.of("/")
    val flags: Seq[CopyOption]        = Seq(StandardCopyOption.COPY_ATTRIBUTES)

    for {
      (fullPath, relativePath) <- initLocation(destStorage.project, destStorage.value, destDesc.uuid, destDesc.filename)
      createdFilePath          <- fs2.io.file.copy[IO](blocker, source, dest, flags)
      _                        <- IO.raiseUnless(createdFilePath == fullPath)(new Exception("Created file not consistent with path"))
    } yield FileAttributes(
      uuid = destDesc.uuid,
      location = Uri(fullPath.toUri.toString),
      path = Uri.Path(relativePath.toString),
      filename = destDesc.filename,
      mediaType = destDesc.mediaType,
      bytes = sourceAttr.bytes,
      digest = sourceAttr.digest,
      origin = Client
    )
  }

  def copyRemote(
      sourcePath: Uri.Path,
      destDesc: FileDescription
  ): IO[FileAttributes] = ???

}
