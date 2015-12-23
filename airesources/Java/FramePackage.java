import java.util.ArrayList;

public class FramePackage
{
    public Map map;
    public ArrayList<Message> messages;
    
    public FramePackage(Map map, ArrayList<Message> messages) {
        this.map = map;
        this.messages = messages;
    }
}
