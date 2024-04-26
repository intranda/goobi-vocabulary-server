package io.goobi.vocabulary.model.jpa;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.proxy.HibernateProxy;

import java.util.LinkedList;
import java.util.List;

@Entity
@Table(name = "field_definition",
        uniqueConstraints = {
        @UniqueConstraint(columnNames = {"schema_id", "label"}),
        @UniqueConstraint(columnNames = {"schema_id", "main_entry"})
})
@Getter
@Setter
public class FieldDefinitionEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private long id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "schema_id", nullable = false)
    private VocabularySchemaEntity schema;

    // `name` is a reserved Mysql keyword
    @Column(name = "label", nullable = false)
    private String name;

    @ManyToOne(optional = false)
    @JoinColumn(name = "type_id", nullable = false)
    private FieldTypeEntity type;

    @OneToMany(mappedBy = "fieldDefinition", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<TranslationDefinitionEntity> translationDefinitions = new LinkedList<>();

    @Column(name = "required", nullable = false)
    private boolean required = false;

    // `unique` is a reserved MariaDB keyword
    @Column(name = "distinctive", nullable = false)
    private boolean unique = false;

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
    private boolean titleField = false;

    @Column(name = "multi_valued", nullable = false)
    private boolean multiValued = false;

    @Override
    public final boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null) {
            return false;
        }
        Class<?> oEffectiveClass = o instanceof HibernateProxy ? ((HibernateProxy) o).getHibernateLazyInitializer().getPersistentClass() : o.getClass();
        Class<?> thisEffectiveClass = this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass() : this.getClass();
        if (thisEffectiveClass != oEffectiveClass) {
            return false;
        }
        FieldDefinitionEntity that = (FieldDefinitionEntity) o;
        return id != that.id;
    }

    @Override
    public final int hashCode() {
        return this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass().hashCode() : getClass().hashCode();
    }
}
