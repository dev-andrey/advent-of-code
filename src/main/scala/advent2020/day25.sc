val cardPublicKey = 10943862
val doorPublicKey = 12721030

val subject = 7L
val divisor = 20201227

@annotation.tailrec
def findLoop(loop: Int, value: Long, cardLoop: Long, doorLoop: Long): (Long, Long) =
  if (cardLoop > 0 && doorLoop > 0) (cardLoop + 1, doorLoop + 1)
  else
    (value * subject) % divisor match {
      case nextSubj if nextSubj == cardPublicKey => findLoop(loop + 1, nextSubj, loop, doorLoop)
      case nextSubj if nextSubj == doorPublicKey => findLoop(loop + 1, nextSubj, cardLoop, loop)
      case nextSubj                              => findLoop(loop + 1, nextSubj, cardLoop, doorLoop)
    }

val (cardLoop, doorLoop) = findLoop(0, 1, -1, -1)

@annotation.tailrec
def findEncryptionKey(loop: Long, encryptionKey: Long, publicKey: Long): Long =
  if (loop == 0) encryptionKey
  else findEncryptionKey(loop - 1, encryptionKey * publicKey % divisor, publicKey)

if (cardLoop > doorLoop) findEncryptionKey(cardLoop, 1, doorPublicKey)
else findEncryptionKey(doorLoop, 1, cardPublicKey)
