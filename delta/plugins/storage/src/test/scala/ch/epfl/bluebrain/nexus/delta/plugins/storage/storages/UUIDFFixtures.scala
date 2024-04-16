package ch.epfl.bluebrain.nexus.delta.plugins.storage.storages

import ch.epfl.bluebrain.nexus.delta.kernel.utils.UUIDF

import java.util.UUID

object UUIDFFixtures {
  trait Random {
    val randomUuid                       = UUID.randomUUID()
    implicit val fixedRandomUuidF: UUIDF = UUIDF.fixed(randomUuid)
  }
}
