package ch.epfl.bluebrain.nexus.delta.sdk.projects.model

import ch.epfl.bluebrain.nexus.delta.rdf.IriOrBNode.Iri
import ch.epfl.bluebrain.nexus.delta.sdk.model.IdSegmentRef
import ch.epfl.bluebrain.nexus.delta.sourcing.model.ResourceRef

/**
  * Defines the context applied within a project
  * @param apiMappings
  *   the API mappings
  * @param base
  *   the base Iri for generated resource IDs
  * @param vocab
  *   an optional vocabulary for resources with no context
  */
final case class ProjectContext(apiMappings: ApiMappings, base: ProjectBase, vocab: Iri)

object ProjectContext {

  def unsafe(apiMappings: ApiMappings, base: Iri, vocab: Iri): ProjectContext =
    ProjectContext(apiMappings, ProjectBase(base), vocab)

  def segmentToResourceRef(projectContext: ProjectContext, segment: IdSegmentRef): Option[ResourceRef] =
    segment.value.toIri(projectContext.apiMappings, projectContext.base).map(expandSegmentWithIri(segment, _))

  def expandSegmentWithIri(segment: IdSegmentRef, iri: Iri): ResourceRef = segment match {
    case IdSegmentRef.Latest(_) => ResourceRef.Latest(iri)
    case IdSegmentRef.Revision(_, rev) => ResourceRef.Revision(iri, rev)
    case IdSegmentRef.Tag(_, tag) => ResourceRef.Tag(iri, tag)
  }
}
