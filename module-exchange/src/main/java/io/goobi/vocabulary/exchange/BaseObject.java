package io.goobi.vocabulary.exchange;

import lombok.Data;

import java.util.Map;

@Data
public abstract class BaseObject {
    private Map<String, HateoasHref> _links;
}
