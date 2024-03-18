package io.goobi.vocabularyserver.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.Getter;

@Entity
@Getter
public class FieldDefinition {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Long id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "schema_id", nullable = false)
    private Schema schema;

    @Column(name = "language", length = 3)
    private String language;

    @Column(name = "name", nullable = false, unique = true)
    private String name;

    @ManyToOne(optional = false)
    @JoinColumn(name = "type_id")
    private FieldType type;

    @ManyToOne
    @JoinColumn(name = "validation_id")
    private FieldValidation validation;

    @Column(name = "required", nullable = false)
    private Boolean required = false;

    @Column(name = "unique", nullable = false)
    private Boolean unique = false;

    @Column(name = "main_entry", nullable = false)
    private Boolean mainEntry = false;

    @Column(name = "title_field", nullable = false)
    private Boolean titleField = false;
}
