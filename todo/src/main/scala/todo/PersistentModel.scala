package todo

import cats.implicits.*
import java.nio.file.{Path, Paths, Files}
import java.nio.charset.StandardCharsets
import io.circe.{Decoder, Encoder}
import io.circe.parser.*
import io.circe.syntax.*
import scala.collection.mutable
import todo.data.*
import todo.PersistentModel.loadTasks
import todo.PersistentModel.loadId

/**
 * The PersistentModel is a model that saves all data to files, meaning that
 * tasks persist between restarts.
 *
 * You should modify this file.
 */
object PersistentModel extends Model:
  import Codecs.given

  /** Path where the tasks are saved */
  val tasksPath = Paths.get("tasks.json")
  /** Path where the next id is saved */
  val idPath = Paths.get("id.json")

  /**
   * Load Tasks from a file. Return an empty task list if the file does not exist,
   * and throws an exception if decoding the file fails.
   */
  def loadTasks(): Tasks =
    if Files.exists(tasksPath) then
      load[Tasks](tasksPath)
    else
      Tasks.empty

  /**
   * Load an Id from a file. This Id is guaranteed to have never been used before.
   * Returns Id(0) if the file does not exist, and throws
   * an exception if decoding the file fails.
   */
  def loadId(): Id =
    if Files.exists(idPath) then
      load[Id](idPath)
    else
      Id(0)

  /**
   * Load JSON-encoded data from a file.
   *
   * Given a file name, load JSON data from that file, and decode it into the
   * type A. Throws an exception on failure.
   *
   * It is not necessary to use this method. You should be able to use loadTasks
   * and loadId instead, which have a simpler interface.
   */
  def load[A](path: Path)(using decoder: Decoder[A]): A = {
    val str = Files.readString(path, StandardCharsets.UTF_8)

    // In a production system we would want to pay more attention to error
    // handling than we do here, but this is sufficient for the case study.
    decode[A](str) match {
      case Right(result) => result
      case Left(error) => throw error
    }
  }

  /**
   * Save tasks to a file. If the file already exists it is overwritten.
   */
  def saveTasks(tasks: Tasks): Unit =
    save(tasksPath, tasks)

  /**
   * Save Id to a file. The Id saved to a file must be an Id that was never used before.
   * If the file already exists it is overwritten.
   */
  def saveId(id: Id): Unit =
    save(idPath, id)
      

  /**
   * Save data to a file in JSON format.
   *
   * Given a file name and some data, saves that data to the file in JSON
   * format. If the file already exists it is overwritten.
   *
   * It is not necessary to use this method. You should be able to use saveTasks
   * and saveId instead, which have a simpler interface.
   */
  def save[A](path: Path, data: A)(using encoder: Encoder[A]): Unit =
    val json = data.asJson
    Files.writeString(path, json.spaces2, StandardCharsets.UTF_8)
    ()

  /* Hint: there are two pieces of state we need to implement the model:
   * - the tasks
   * - the next Id
   * (The InMemoryModel uses the same.)
   */

  private val idGenerator = IdGenerator(loadId())

  private val idStore: mutable.LinkedHashMap[Id, Task] =
    mutable.LinkedHashMap.from(loadTasks().tasks.map(task => task))
    
  def create(task: Task): Id =
    val id = idGenerator.nextId() 
    idStore += (id -> task) // Añadir la id con tarea a la idStore
    saveTasks(tasks) //guardar la tarea al crearla en la ruta con la que está el archivo con las tareas
    saveId(id) //guardar la id en la función donde busca el archivo con las id guardadas
    id

  def read(id: Id): Option[Task] =
    idStore.get(id) 

  def update(id: Id)(f: Task => Task): Option[Task] =
    idStore.updateWith(id) {
      case Some(task) => Some(f(task))
      case None => None //actualizar tarea, en caso de que haya alguna actualizarla, sino no
    }

  def delete(id: Id): Boolean =
    idStore.remove(id).isDefined

  def tasks: Tasks =
    Tasks(idStore.toList) //listar tareas

  def tasks(tag: Tag): Tasks =
    Tasks(idStore.filter { case(_,task)=> task.tags.contains(tag) }.toList)  //filtrar las tareas que tienen el filtro

  def complete(id: Id): Option[Task] =
    idStore.get(id).map { task => 
      val updatedTask = task.copy(state = State.completedNow)
      idStore.update(id, updatedTask)
      updatedTask //hacer una copia del estado y actualizarlo a completado, si se vuelve a dar, se desmarcará como completo
    }

  def tags: Tags =
    Tags(idStore.values.flatMap(_.tags).toList.distinct)

  /**
  * Delete the tasks and id files if they exist.
  */
  def clear(): Unit =
    idStore.clear()
