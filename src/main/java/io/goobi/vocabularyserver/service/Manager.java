package io.goobi.vocabularyserver.service;

import java.util.List;

public interface Manager<T> {
    List<T> listAll();
    T create(T newT);
}
