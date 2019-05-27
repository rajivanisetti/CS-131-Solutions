import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v) { convertFromByteArray(v); maxval = 127; }

    GetNSetState(byte[] v, byte m) { convertFromByteArray(v); maxval = m; }

    private void convertFromByteArray(byte [] v)
    {
        value = new AtomicIntegerArray(v.length);
        for (int i = 0; i < v.length; i++)
            value.set(i, v[i]);
    }

    public byte[] current()
    {
        int size = value.length();
        byte[] v = new byte[size];
        for (int i = 0; i < size; i++)
            v[i] = (byte) value.get(i);

        return v;
    }

    public int size() { return value.length(); }

    public boolean swap(int i, int j) 
    {
        int val_i = value.get(i);
        int val_j = value.get(j);
	   
        if (val_i <= 0 || val_j >= maxval)
            return false;
	   
        value.set(i, val_i - 1);
	    value.set(j, val_j + 1);
	    return true;
    }
}
