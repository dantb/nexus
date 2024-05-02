package ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.s3

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpEntity, Uri}
import cats.effect.IO
import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.model.Digest.ComputedDigest
import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.model.FileAttributes.FileAttributesOrigin
import ch.epfl.bluebrain.nexus.delta.plugins.storage.files.model.FileStorageMetadata
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.StoragesConfig.S3StorageConfig
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.model.DigestAlgorithm
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.model.Storage.S3Storage
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.model.StorageRejection.StorageNotAccessible
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.model.StorageValue.S3StorageValue
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.AkkaSourceHelpers
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.operations.s3.client.S3StorageClient
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.permissions.{read, write}
import ch.epfl.bluebrain.nexus.delta.plugins.storage.storages.{StorageFixtures, UUIDFFixtures}
import ch.epfl.bluebrain.nexus.delta.rdf.syntax.iriStringContextSyntax
import ch.epfl.bluebrain.nexus.delta.sdk.actor.ActorSystemSetup
import ch.epfl.bluebrain.nexus.delta.sourcing.model.ProjectRef
import ch.epfl.bluebrain.nexus.testkit.mu.NexusSuite
import io.circe.Json
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import munit.AnyFixture
import org.apache.commons.codec.binary.Hex

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.concurrent.duration.{Duration, DurationInt}

class S3FileOperationsSuite
    extends NexusSuite
    with StorageFixtures
    with UUIDFFixtures.Random
    with ActorSystemSetup.Fixture
    with LocalStackS3StorageClient.Fixture
    with AkkaSourceHelpers
    with S3Helpers {

  override def munitIOTimeout: Duration = 120.seconds

  override def munitFixtures: Seq[AnyFixture[_]] = List(localStackS3Client, actorSystem)

  implicit private lazy val (s3StorageClient: S3StorageClient, underlying: S3AsyncClientOp[IO], conf: S3StorageConfig) =
    localStackS3Client()
  implicit private lazy val as: ActorSystem                                                                            = actorSystem()

  private lazy val fileOps = S3FileOperations.mk(s3StorageClient)

  private def makeContentHash(algorithm: DigestAlgorithm, content: String) = {
    Hex.encodeHexString(MessageDigest.getInstance(algorithm.value).digest(content.getBytes(StandardCharsets.UTF_8)))
  }

  private def expectedPath(proj: ProjectRef, filename: String): Uri.Path =
    Uri.Path(s"${conf.prefix}/$proj/${randomUuid.toString.takeWhile(_ != '-').mkString("/")}/$filename")

  private def expectedLocation(bucket: String, proj: ProjectRef, filename: String): Uri =
    s"${conf.defaultEndpoint}/$bucket/${conf.prefix}/$proj/${randomUuid.toString.takeWhile(_ != '-').mkString("/")}/$filename"

  test("List objects in an existing bucket") {
    givenAnS3Bucket { bucket =>
      fileOps.checkBucketExists(bucket)
    }
  }

  test("Fail to list objects when bucket doesn't exist") {
    fileOps.checkBucketExists(genString()).intercept[StorageNotAccessible]
  }

  test("Save and fetch an object in a bucket") {
    givenAnS3Bucket { bucket =>
      val storageValue = S3StorageValue(
        default = false,
        algorithm = DigestAlgorithm.default,
        bucket = bucket,
        readPermission = read,
        writePermission = write,
        maxFileSize = 20
      )

      val iri     = iri"http://localhost/s3"
      val project = ProjectRef.unsafe("org", "project")
      val storage = S3Storage(iri, project, storageValue, Json.obj())

      val filename      = "myfile.txt"
      val content       = genString()
      val hashOfContent = makeContentHash(DigestAlgorithm.default, content)
      val entity        = HttpEntity(content)

      val expectedMetadata =
        FileStorageMetadata(
          randomUuid,
          content.length.toLong,
          ComputedDigest(DigestAlgorithm.default, hashOfContent),
          FileAttributesOrigin.Client,
          expectedLocation(bucket, project, filename),
          expectedPath(project, filename)
        )

      val result = for {
        storageMetadata <- fileOps.save(storage, filename, entity)
        _                = assertEquals(storageMetadata, expectedMetadata)
        source          <- fileOps.fetch(bucket, storageMetadata.path)
      } yield consume(source)

      assertIO(result, content)
    }
  }

  test("Use SHA-1 to calculate a checksum") {
    givenAnS3Bucket { bucket =>
      val storageValue = S3StorageValue(
        default = false,
        algorithm = DigestAlgorithm.SHA1,
        bucket = bucket,
        readPermission = read,
        writePermission = write,
        maxFileSize = 20
      )

      val iri     = iri"http://localhost/s3"
      val project = ProjectRef.unsafe("org", "project")
      val storage = S3Storage(iri, project, storageValue, Json.obj())

      val filename      = "myfile.txt"
      val content       = genString()
      val hashOfContent = makeContentHash(DigestAlgorithm.SHA1, content)
      val entity        = HttpEntity(content)

      for {
        attr <- fileOps.save(storage, filename, entity)
        _     = assertEquals(attr.digest, ComputedDigest(DigestAlgorithm.SHA1, hashOfContent))
      } yield ()
    }
  }
}
