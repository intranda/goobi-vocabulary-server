package io.goobi.vocabularyserver.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;

@Entity
@Getter
@RequiredArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(uniqueConstraints = {@UniqueConstraint(columnNames = {"schema_id", "name"}), @UniqueConstraint(columnNames = {"schema_id", "main_entry"})})
public class FieldDefinition {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Long id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "schema_id", nullable = false)
    @NonNull
    private VocabularySchema schema;

    @Column(name = "language", length = 3)
    private String language;

    @Column(name = "name", nullable = false)
    @NonNull
    private String name;

    @ManyToOne(optional = false)
    @JoinColumn(name = "type_id")
    @NonNull
    private FieldType type;

    @Column(name = "required", nullable = false)
    @Setter
    @NonNull
    private Boolean required = false;

    @Column(name = "distinctive", nullable = false)
    @Setter
    @NonNull
    private Boolean unique = false;

    @Column(name = "main_entry")
    private Boolean mainEntry;

    public void setMainEntry(final Boolean newValue) {
        if (Boolean.TRUE.equals(newValue)) {
            this.mainEntry = true;
        } else {
            this.mainEntry = null;
        }
    }

    @Column(name = "title_field", nullable = false)
    @Setter
    @NonNull
    private Boolean titleField = false;
}
