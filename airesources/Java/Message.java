public class Message
{
    MessageType type;
    public int senderID, recipientID, targetID;
    
    public Message(MessageType type, int senderID, int recipientID, int targetID) {
        this.type = type;
        this.senderID = senderID;
        this.recipientID = recipientID;
        this.targetID = targetID;
    }
}
