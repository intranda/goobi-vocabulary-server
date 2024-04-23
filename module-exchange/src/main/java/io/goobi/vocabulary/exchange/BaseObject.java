package io.goobi.vocabulary.exchange;

import lombok.Data;

import java.util.Map;

@Data
public abstract class BaseObject implements Identifiable {
    private Map<String, HateoasHref> _links;
}
