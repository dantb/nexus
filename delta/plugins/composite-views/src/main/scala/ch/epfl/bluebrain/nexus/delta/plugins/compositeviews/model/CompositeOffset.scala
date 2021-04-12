package ch.epfl.bluebrain.nexus.delta.plugins.compositeviews.model

import akka.persistence.query.Offset
import ch.epfl.bluebrain.nexus.delta.sourcing.projections.instances._
import ch.epfl.bluebrain.nexus.delta.rdf.IriOrBNode.Iri
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/**
  * An offset for a composite view projection
  *
  * @param sourceId     the Iri of the composite view source
  * @param projectionId the Iri of the composite view projection
  * @param offset       the offset value
  */
final case class CompositeOffset(sourceId: Iri, projectionId: Iri, offset: Offset)

object CompositeOffset {
  implicit val compositeOffsetSort: Ordering[CompositeOffset] =
    Ordering.by[CompositeOffset, String](_.sourceId.toString).orElseBy(_.projectionId.toString)

  implicit val compositeOffsetEncoder: Encoder.AsObject[CompositeOffset] = deriveEncoder[CompositeOffset]

}