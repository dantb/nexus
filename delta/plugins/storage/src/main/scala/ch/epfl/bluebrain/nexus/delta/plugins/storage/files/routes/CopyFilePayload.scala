package ch.epfl.bluebrain.nexus.delta.plugins.storage.files.routes

import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.model.FileId
import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.model.FileRejection.InvalidFileLookup
import ch.epfl.bluebrain.nexus.delta.sdk.model.IdSegment
import ch.epfl.bluebrain.nexus.delta.sourcing.model.ProjectRef
import ch.epfl.bluebrain.nexus.delta.sourcing.model.Tag.UserTag
import io.circe.Decoder

final case class CopyFilePayload(
    destFilename: Option[String],
    sourceProj: ProjectRef,
    sourceFile: IdSegment,
    sourceTag: Option[UserTag],
    sourceRev: Option[Int]
) {
  def toSourceFileId: Either[InvalidFileLookup, FileId] = (sourceTag, sourceRev) match {
    case (Some(tag), None)  => Right(FileId(sourceFile, tag, sourceProj))
    case (None, Some(rev))  => Right(FileId(sourceFile, rev, sourceProj))
    case (None, None)       => Right(FileId(sourceFile, sourceProj))
    case (Some(_), Some(_)) => Left(InvalidFileLookup(sourceFile))
  }
}

object CopyFilePayload {

  implicit val dec: Decoder[CopyFilePayload] = Decoder.instance { cur =>
    val source = cur.downField("source")
    for {
      destFilename <- cur.get[Option[String]]("destinationFilename")
      sourceProj   <- source.get[ProjectRef]("projectRef")
      sourceFileId <- source.get[String]("fileId").map(IdSegment(_))
      sourceTag    <- source.get[Option[UserTag]]("tag")
      sourceRev    <- source.get[Option[Int]]("rev")
    } yield CopyFilePayload(destFilename, sourceProj, sourceFileId, sourceTag, sourceRev)
  }
}