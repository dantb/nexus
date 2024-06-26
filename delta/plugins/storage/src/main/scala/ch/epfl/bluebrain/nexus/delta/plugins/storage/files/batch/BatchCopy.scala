package ch.epfl.bluebrain.nexus.delta.plugins.storage.files.batch

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.toFunctorOps
import ch.epfl.bluebrain.nexus.delta.kernel.utils.UUIDF
import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.FetchFileResource
import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.model._
import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.routes.CopyFileSource
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.FetchStorage
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.model.Storage.{DiskStorage, RemoteDiskStorage}
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.model.{Storage, StorageType}
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.StorageFileRejection.CopyFileRejection
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.StorageFileRejection.CopyFileRejection._
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.disk.{DiskCopyDetails, DiskStorageCopyFiles}
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.remote.RemoteDiskStorageCopyFiles
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.remote.client.model.RemoteDiskCopyDetails
import ch.epfl.bluebrain.nexus.delta.sdk.acls.AclCheck
import ch.epfl.bluebrain.nexus.delta.sdk.error.ServiceError.AuthorizationFailed
import ch.epfl.bluebrain.nexus.delta.sdk.identities.model.Caller
import shapeless.syntax.typeable.typeableOps

trait BatchCopy {
  def copyFiles(source: CopyFileSource, destStorage: Storage)(implicit
      c: Caller
  ): IO[NonEmptyList[FileAttributes]]
}

object BatchCopy {
  def mk(
      fetchFile: FetchFileResource,
      fetchStorage: FetchStorage,
      aclCheck: AclCheck,
      diskCopy: DiskStorageCopyFiles,
      remoteDiskCopy: RemoteDiskStorageCopyFiles
  )(implicit uuidF: UUIDF): BatchCopy = new BatchCopy {

    override def copyFiles(source: CopyFileSource, destStorage: Storage)(implicit
        c: Caller
    ): IO[NonEmptyList[FileAttributes]] =
      destStorage match {
        case disk: Storage.DiskStorage         => copyToDiskStorage(source, disk)
        case remote: Storage.RemoteDiskStorage => copyToRemoteStorage(source, remote)
        case s3: Storage.S3Storage             => unsupported(s3.tpe)
      }

    private def copyToRemoteStorage(source: CopyFileSource, dest: RemoteDiskStorage)(implicit c: Caller) =
      for {
        remoteCopyDetails <- source.files.traverse(fetchRemoteCopyDetails(dest, _))
        _                 <- validateFilesForStorage(dest, remoteCopyDetails.map(_.sourceMetadata.bytes))
        attributes        <- remoteDiskCopy.copyFiles(dest, remoteCopyDetails)
      } yield {
        attributes
      }

    private def copyToDiskStorage(source: CopyFileSource, dest: DiskStorage)(implicit
        c: Caller
    ): IO[NonEmptyList[FileAttributes]] =
      for {
        diskCopyDetails <- source.files.traverse(fetchDiskCopyDetails(dest, _))
        _               <- validateFilesForStorage(dest, diskCopyDetails.map(_.sourceAttributes.bytes))
        destAttributes  <- diskCopy.copyFiles(dest, diskCopyDetails)
      } yield {
        destAttributes
      }

    private def validateFilesForStorage(destStorage: Storage, sourcesBytes: NonEmptyList[Long]): IO[Unit] = {
      val maxSize = destStorage.storageValue.maxFileSize
      IO.raiseWhen(sourcesBytes.exists(_ > maxSize))(SourceFileTooLarge(maxSize, destStorage.id))
    }

    private def fetchDiskCopyDetails(destStorage: DiskStorage, fileId: FileId)(implicit
        c: Caller
    ): IO[DiskCopyDetails] =
      for {
        (file, sourceStorage) <- fetchFileAndValidateStorage(fileId)
        _                     <- validateDiskStorage(destStorage, sourceStorage)
      } yield DiskCopyDetails(destStorage, file.attributes)

    private def validateDiskStorage(destStorage: DiskStorage, sourceStorage: Storage) =
      sourceStorage
        .narrowTo[DiskStorage]
        .as(IO.unit)
        .getOrElse(differentStorageTypeError(destStorage, sourceStorage))

    private def fetchRemoteCopyDetails(destStorage: RemoteDiskStorage, fileId: FileId)(implicit c: Caller) =
      for {
        (file, sourceStorage) <- fetchFileAndValidateStorage(fileId)
        sourceBucket          <- validateRemoteStorage(destStorage, sourceStorage)
        uuid                  <- uuidF()
      } yield RemoteDiskCopyDetails(
        uuid,
        destStorage,
        file.attributes.path,
        sourceBucket,
        FileMetadata.from(file.attributes),
        FileDescription.from(file)
      )

    private def validateRemoteStorage(destStorage: RemoteDiskStorage, sourceStorage: Storage) =
      sourceStorage
        .narrowTo[RemoteDiskStorage]
        .map(remote => IO.pure(remote.value.folder))
        .getOrElse(differentStorageTypeError(destStorage, sourceStorage))

    private def differentStorageTypeError[A](destStorage: Storage, sourceStorage: Storage) =
      IO.raiseError[A](DifferentStorageTypes(sourceStorage.id, sourceStorage.tpe, destStorage.tpe))

    private def unsupported(tpe: StorageType) = IO.raiseError(CopyFileRejection.UnsupportedOperation(tpe))

    private def fetchFileAndValidateStorage(id: FileId)(implicit c: Caller) = {
      for {
        file          <- fetchFile.fetch(id)
        sourceStorage <- fetchStorage.fetch(file.value.storage, id.project)
        perm           = sourceStorage.value.storageValue.readPermission
        _             <- aclCheck.authorizeForOr(id.project, perm)(AuthorizationFailed(id.project, perm))
      } yield (file.value, sourceStorage.value)
    }
  }

}
