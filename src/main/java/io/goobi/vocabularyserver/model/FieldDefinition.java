package io.goobi.vocabularyserver.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

@Entity
@Getter
@RequiredArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class FieldDefinition {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Long id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "schema_id", nullable = false)
    @NonNull
    private Schema schema;

    @Column(name = "language", length = 3)
    private String language;

    @Column(name = "name", nullable = false, unique = true)
    @NonNull
    private String name;

    @ManyToOne(optional = false)
    @JoinColumn(name = "type_id")
    @NonNull
    private FieldType type;

    @ManyToOne
    @JoinColumn(name = "validation_id")
    private FieldValidation validation;

    @Column(name = "required", nullable = false)
    @NonNull
    private Boolean required = false;

    @Column(name = "unique", nullable = false)
    @NonNull
    private Boolean unique = false;

    @Column(name = "main_entry", nullable = false)
    @NonNull
    private Boolean mainEntry = false;

    @Column(name = "title_field", nullable = false)
    @NonNull
    private Boolean titleField = false;
}
