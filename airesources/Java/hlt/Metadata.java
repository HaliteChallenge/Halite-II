package hlt;

public class Metadata {
    final private String[] metadata;
    private int index = 0;

    public Metadata(final String[] metadata) {
        this.metadata = metadata;
    }

    public String pop() {
        return metadata[index++];
    }

    public boolean isEmpty() {
        return index == metadata.length;
    }
}
