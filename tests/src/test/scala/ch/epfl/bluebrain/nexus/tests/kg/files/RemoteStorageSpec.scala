package ch.epfl.bluebrain.nexus.tests.kg.files

import akka.http.scaladsl.model.{ContentTypes, MediaTypes, StatusCodes}
import akka.util.ByteString
import cats.effect.IO
import ch.epfl.bluebrain.nexus.tests.HttpClient._
import ch.epfl.bluebrain.nexus.tests.Identity.storages.Coyote
import ch.epfl.bluebrain.nexus.tests.Optics.{filterKey, filterMetadataKeys, projections}
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission.Supervision
import ch.epfl.bluebrain.nexus.tests.kg.files.model.FileInput
import io.circe.generic.semiauto.deriveDecoder
import io.circe.syntax.KeyOps
import io.circe.{Decoder, Json}
import org.scalatest.Assertion

class RemoteStorageSpec extends StorageSpec {

  override def storageName: String = "external"

  override def storageType: String = "RemoteDiskStorage"

  override def storageId: String = "myexternalstorage"

  override def locationPrefix: Option[String] = Some(s"file:///tmp/$remoteFolder")

  private val remoteFolder = genId()

  override def beforeAll(): Unit = {
    super.beforeAll()
    storagesDsl.mkProtectedFolderInStorageService(remoteFolder).accepted
  }

  override def afterAll(): Unit = {
    super.afterAll()
    storagesDsl.deleteFolderInStorageService(remoteFolder).accepted
  }

  private def storageResponse(project: String, id: String, readPermission: String, writePermission: String) =
    jsonContentOf(
      "kg/storages/remote-disk-response.json",
      replacements(
        Coyote,
        "folder"      -> remoteFolder,
        "id"          -> id,
        "project"     -> project,
        "self"        -> storageSelf(project, s"https://bluebrain.github.io/nexus/vocabulary/$id"),
        "maxFileSize" -> storageConfig.maxFileSize.toString,
        "read"        -> readPermission,
        "write"       -> writePermission
      ): _*
    )

  override def createStorages(projectRef: String, storId: String, storName: String): IO[Assertion] = {
    val storageId2    = s"${storId}2"
    val storage2Read  = s"$storName/read"
    val storage2Write = s"$storName/write"

    val expectedStorage          = storageResponse(projectRef, storId, "resources/read", "files/write")
    val expectedStorageSource    =
      jsonContentOf(
        "kg/storages/storage-source.json",
        "folder"      -> remoteFolder,
        "storageBase" -> StoragesDsl.StorageServiceBaseUrl,
        "id"          -> storId
      )
    val expectedStorageWithPerms = storageResponse(projectRef, storageId2, storage2Read, storage2Write)

    for {
      _ <- storagesDsl.createRemoteStorageWithDefaultPerms(storId, projectRef, remoteFolder)
      _ <- storagesDsl.checkStorageMetadata(projectRef, storId, expectedStorage)
      _ <- storagesDsl.checkStorageSource(projectRef, storId, expectedStorageSource)
      _ <- permissionDsl.addPermissions(Permission(storName, "read"), Permission(storName, "write"))
      _ <- storagesDsl.createRemoteStorageWithCustomPerms(
             storageId2,
             projectRef,
             remoteFolder,
             storage2Read,
             storage2Write
           )
      _ <- storagesDsl.checkStorageMetadata(projectRef, storageId2, expectedStorageWithPerms)
    } yield succeed
  }

  def putFile(name: String, content: String, storageId: String) = {
    val file = FileInput(s"test-resource:$name", name, ContentTypes.`text/plain(UTF-8)`, content)
    deltaClient.uploadFile(projectRef, storageId, file, None) { expectCreated }
  }

  def randomString(length: Int) = {
    val r  = new scala.util.Random
    val sb = new StringBuilder
    for (_ <- 1 to length) {
      sb.append(r.nextPrintableChar())
    }
    sb.toString
  }

  case class CustomMetadata(name: String, description: String, keywords: Map[String, String])
  private val customMetadata =
    CustomMetadata("cool name", "good description", Map("key1" -> "value1", "key2" -> "value2"))

  "succeed many large files are in the archive, going over the time limit" ignore {
    val content = randomString(130000000)
    val payload = jsonContentOf("kg/archives/archive-many-large-files.json")
    var before  = 0L
    for {
      _ <- putFile("largefile1.txt", content, s"${storageId}2")
      _ <- putFile("largefile2.txt", content, s"${storageId}2")
      _ <- putFile("largefile3.txt", content, s"${storageId}2")
      _ <- putFile("largefile4.txt", content, s"${storageId}2")
      _ <- putFile("largefile5.txt", content, s"${storageId}2")
      _ <- putFile("largefile6.txt", content, s"${storageId}2")
      _ <- putFile("largefile7.txt", content, s"${storageId}2")
      _ <- putFile("largefile8.txt", content, s"${storageId}2")
      _ <- putFile("largefile9.txt", content, s"${storageId}2")
      _ <- putFile("largefile10.txt", content, s"${storageId}2")
      _ <- putFile("largefile11.txt", content, s"${storageId}2")
      _ <- putFile("largefile12.txt", content, s"${storageId}2")
      _ <- putFile("largefile13.txt", content, s"${storageId}2")
      _ <- putFile("largefile14.txt", content, s"${storageId}2")
      _ <- putFile("largefile15.txt", content, s"${storageId}2")
      _ <-
        deltaClient.put[ByteString](s"/archives/$projectRef/nxv:very-large-archive", payload, Coyote) { (_, response) =>
          before = System.currentTimeMillis()
          response.status shouldEqual StatusCodes.Created
        }
      _ <-
        deltaClient.get[ByteString](s"/archives/$projectRef/nxv:very-large-archive", Coyote, acceptAll) {
          (_, response) =>
            println(s"time taken to download archive: ${System.currentTimeMillis() - before}ms")
            response.status shouldEqual StatusCodes.OK
            contentType(response) shouldEqual MediaTypes.`application/zip`.toContentType
        }
    } yield {
      succeed
    }
  }

  "Creating a remote storage" should {
    "fail creating a RemoteDiskStorage without folder" in {
      val payload = storagesDsl.remoteDiskPayloadDefaultPerms(storageId, "nexustest").accepted

      deltaClient.post[Json](s"/storages/$projectRef", filterKey("folder")(payload), Coyote) { (_, response) =>
        response.status shouldEqual StatusCodes.BadRequest
      }
    }
  }

  def createFileInStorageService(filename: String): IO[Unit] =
    storagesDsl.runCommandInStorageService(s"echo 'file content' > /tmp/$remoteFolder/$filename")

  def linkPayload(filename: String, path: String, mediaType: Option[String]) =
    Json.obj(
      "filename"  := filename,
      "path"      := path,
      "mediaType" := mediaType
    )

  private def linkPayloadWithMetadata(
      filename: String,
      path: String,
      md: CustomMetadata
  ) =
    Json.obj(
      "filename"  := filename,
      "path"      := path,
      "mediaType" := None,
      "metadata"  := Json.obj(
        "name"        := md.name,
        "description" := md.description,
        "keywords"    := md.keywords
      )
    )

  def linkFile(payload: Json)(fileId: String, filename: String, mediaType: Option[String]) = {
    val expected = jsonContentOf(
      "kg/files/remote-linked.json",
      replacements(
        Coyote,
        "id"          -> fileId,
        "self"        -> fileSelf(projectRef, fileId),
        "filename"    -> filename,
        "mediaType"   -> mediaType.orNull,
        "storageId"   -> s"${storageId}2",
        "storageType" -> storageType,
        "projId"      -> s"$projectRef",
        "project"     -> s"${config.deltaUri}/projects/$projectRef"
      ): _*
    )
    deltaClient.put[Json](s"/files/$projectRef/$filename?storage=nxv:${storageId}2", payload, Coyote) {
      (json, response) =>
        filterMetadataKeys.andThen(filterKey("_location"))(json) shouldEqual expected
        response.status shouldEqual StatusCodes.Created
    }
  }

  def fetchUpdatedLinkedFile(fileId: String, filename: String, mediaType: String) = {
    val expected = jsonContentOf(
      "kg/files/remote-updated-linked.json",
      replacements(
        Coyote,
        "id"          -> fileId,
        "self"        -> fileSelf(projectRef, fileId),
        "filename"    -> filename,
        "mediaType"   -> mediaType,
        "storageId"   -> s"${storageId}2",
        "storageType" -> storageType,
        "projId"      -> s"$projectRef",
        "project"     -> s"${config.deltaUri}/projects/$projectRef"
      ): _*
    )

    eventually {
      deltaClient.get[Json](s"/files/$projectRef/$filename", Coyote) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        filterMetadataKeys.andThen(filterKey("_location"))(json) shouldEqual expected
      }
    }
  }

  "Linking a custom file providing a media type for a .custom file" should {

    val filename = "link_file.custom"
    val fileId   = s"${config.deltaUri}/resources/$projectRef/_/$filename"

    "succeed" in {

      val mediaType = "application/json"
      val payload   = linkPayload(filename, filename, Some(mediaType))

      for {
        _ <- createFileInStorageService(filename)
        // Get a first response without the digest
        _ <- linkFile(payload)(fileId, filename, Some(mediaType))
        // Eventually
      } yield succeed
    }

    "fetch eventually a linked file with updated attributes" in {
      val mediaType = "application/json"
      fetchUpdatedLinkedFile(fileId, filename, mediaType)
    }
  }

  "Linking a file without a media type for a .custom file" should {

    val filename = "link_file_no_media_type.custom"
    val fileId   = s"${config.deltaUri}/resources/$projectRef/_/$filename"

    "succeed" in {
      val payload = linkPayload(filename, filename, None)

      for {
        _ <- createFileInStorageService(filename)
        // Get a first response without the digest
        _ <- linkFile(payload)(fileId, filename, None)
        // Eventually
      } yield succeed
    }

    "fetch eventually a linked file with updated attributes detecting application/custom from config" in {
      val mediaType = "application/custom"
      fetchUpdatedLinkedFile(fileId, filename, mediaType)
    }
  }

  "Linking a file without a media type for a .txt file" should {

    val filename = "link_file.txt"
    val fileId   = s"${config.deltaUri}/resources/$projectRef/_/$filename"

    "succeed" in {
      val payload = linkPayload(filename, filename, None)

      for {
        _ <- createFileInStorageService(filename)
        // Get a first response without the digest
        _ <- linkFile(payload)(fileId, filename, None)
        // Eventually
      } yield succeed
    }

    "fetch eventually a linked file with updated attributes detecting text/plain from akka" in {
      val mediaType = "text/plain; charset=UTF-8"
      fetchUpdatedLinkedFile(fileId, filename, mediaType)
    }
  }

  "Linking a file without a media type for a file without extension" should {

    val filename = "link_file_no_extension"
    val fileId   = s"${config.deltaUri}/resources/$projectRef/_/$filename"

    "succeed" in {
      val payload = linkPayload(filename, filename, None)

      for {
        _ <- createFileInStorageService(filename)
        // Get a first response without the digest
        _ <- linkFile(payload)(fileId, filename, None)
        // Eventually
      } yield succeed
    }

    "fetch eventually a linked file with updated attributes falling back to default mediaType" in {
      val mediaType = "application/octet-stream"
      fetchUpdatedLinkedFile(fileId, filename, mediaType)
    }
  }

  "Linking providing a nonexistent file" should {

    "fail" in {
      val payload = linkPayload("logo.png", "non/existent.png", Some("image/png"))

      deltaClient.put[Json](s"/files/$projectRef/nonexistent.png?storage=nxv:${storageId}2", payload, Coyote) {
        (_, response) =>
          response.status shouldEqual StatusCodes.BadRequest
      }
    }

  }

  "Linking a file with custom metadata should" should {

    "succeed" in {
      val filename = s"${genString()}.txt"
      val md       = customMetadata
      val payload  = linkPayloadWithMetadata(filename, filename, md)

      for {
        _ <- createFileInStorageService(filename)
        _ <- linkFile(filename, payload)
        _ <- assertCorrectCustomMetadata(filename, md)
      } yield succeed
    }

    "succeed when updating with metadata" in {
      val filename  = s"${genString()}.txt"
      val filename2 = s"${genString()}.txt"
      val md        = customMetadata

      val simplePayload       = linkPayload(filename, filename, None)
      val payloadWithMetadata = linkPayloadWithMetadata(filename2, filename2, md)

      val setup = for {
        _ <- createFileInStorageService(filename)
        _ <- createFileInStorageService(filename2)
        _ <- linkFile(filename, simplePayload)
      } yield succeed

      setup.accepted
      eventually { assertFileRevision(filename, 2) }
      updateFileLink(filename, payloadWithMetadata).accepted

      eventually { assertCorrectCustomMetadata(filename, md) }
    }

  }

  "The file-attributes-updated projection description" should {
    "exist" in {
      aclDsl.addPermission("/", Coyote, Supervision.Read).accepted
      deltaClient.get[Json]("/supervision/projections", Coyote) { (json, _) =>
        val expected = json"""{ "module": "system", "name": "file-attributes-update" }"""
        assert(projections.metadata.json.exist(_ == expected)(json))
      }
    }

    "have updated progress when a file is updated" in {
      case class SupervisedDescription(metadata: Metadata, progress: ProjectionProgress)
      case class Metadata(module: String, name: String)
      case class ProjectionProgress(processed: Int)

      implicit val metadataDecoder: Decoder[Metadata] = deriveDecoder

      implicit val progressDecoder: Decoder[ProjectionProgress]       = deriveDecoder
      implicit val descriptionDecoder: Decoder[SupervisedDescription] = deriveDecoder

      /**
        * Given a list of supervised descriptions (json), get the number of processed elements for the
        * `file-attribute-update` projection
        */
      def getProcessed(json: Json): Option[Int] = {
        val Right(projections)      = json.hcursor.downField("projections").as[List[SupervisedDescription]]
        val fileAttributeProjection =
          projections.find(p => p.metadata.name == "file-attribute-update" && p.metadata.module == "system")
        fileAttributeProjection.map(_.progress.processed)
      }

      // get progress prior to updating the file
      deltaClient.get[Json]("/supervision/projections", Coyote) { (json1, _) =>
        val file = FileInput("file.txt", "file.txt", ContentTypes.`application/json`, s"""{ "json": "content"}""")
        eventually {
          // update the file
          deltaClient.uploadFile(projectRef, s"${storageId}2", file, Some(2)) { (_, _) =>
            eventually {
              // get progress after the file update and compare
              deltaClient.get[Json]("/supervision/projections", Coyote) { (json2, _) =>
                assert(getProcessed(json2) == getProcessed(json1).map(_ + 1))
              }
            }
          }
        }
      }
    }

  }

  private def linkFile(filename: String, payload: Json) =
    deltaClient.put[Json](s"/files/$projectRef/$filename?storage=nxv:${storageId}2", payload, Coyote) { (_, response) =>
      response.status shouldEqual StatusCodes.Created
    }

  private def assertFileRevision(filename: String, expectedRev: Int) =
    deltaClient
      .get[Json](s"/files/$projectRef/$filename", Coyote) { (json, _) =>
        json.hcursor.get[Int]("_rev").toOption should contain(expectedRev)
      }

  private def updateFileLink(filename: String, payload: Json) =
    deltaClient
      .put[Json](s"/files/$projectRef/$filename?rev=2&storage=nxv:${storageId}2", payload, Coyote) { (_, response) =>
        response.status shouldEqual StatusCodes.OK
      }

  private def assertCorrectCustomMetadata(
      filename: String,
      customMetadata: CustomMetadata
  ) =
    deltaClient
      .get[Json](s"/files/$projectRef/$filename", Coyote) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        json.hcursor.get[String]("name").toOption should contain(customMetadata.name)
        json.hcursor.get[String]("description").toOption should contain(customMetadata.description)
        json.hcursor.get[Map[String, String]]("_keywords").toOption should contain(customMetadata.keywords)
      }

}
