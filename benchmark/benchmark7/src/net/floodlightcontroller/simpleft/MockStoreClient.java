package net.floodlightcontroller.simpleft;

import org.sdnplatform.sync.IStoreClient;
import org.sdnplatform.sync.IVersion;

public class MockStoreClient<K, V> implements IStoreClient<K, V> {
    public boolean error = false;

    @Override
    public V getValue(K key) throws Exception {
        return null;
    }

    @Override
    public IVersion put(K key, V value) {
        if (value == null) error = true;
        return null;
    }

    @Override
    public boolean getErrorStatus() {
        return error;
    }
}
