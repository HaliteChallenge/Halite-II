public enum MessageType
{
    ATTACK(0), STOP_ATTACK(1);
    
    private final int value;
    private MessageType(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }
    
    public static MessageType getType(int intValue) {
        MessageType[] types = MessageType.values();
        return types[intValue];
    }
}
