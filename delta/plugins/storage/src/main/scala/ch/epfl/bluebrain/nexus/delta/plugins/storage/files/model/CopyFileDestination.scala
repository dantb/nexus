package ch.epfl.bluebrain.nexus.delta.plugins.storage.files.model

import ch.epfl.bluebrain.nexus.delta.sdk.model.IdSegment
import ch.epfl.bluebrain.nexus.delta.sourcing.model.ProjectRef
import ch.epfl.bluebrain.nexus.delta.sourcing.model.Tag.UserTag

final case class CopyFileDestination(
    project: ProjectRef,
    fileId: Option[FileId],
    storage: Option[IdSegment],
    tag: Option[UserTag],
    filename: Option[String]
)
