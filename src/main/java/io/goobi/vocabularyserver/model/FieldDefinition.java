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
import lombok.Getter;
import lombok.Setter;
import org.hibernate.proxy.HibernateProxy;

@Entity
@Getter
@Setter
@Table(uniqueConstraints = {
        @UniqueConstraint(columnNames = {"schema_id", "label"}),
        @UniqueConstraint(columnNames = {"schema_id", "main_entry"})
})
public class FieldDefinition {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private long id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "schema_id", nullable = false)
    private VocabularySchema schema;

    // `name` is a reserved Mysql keyword
    @Column(name = "label", nullable = false)
    private String name;

    @ManyToOne(optional = false)
    @JoinColumn(name = "type_id", nullable = false)
    private FieldType type;

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
        FieldDefinition that = (FieldDefinition) o;
        return id != that.id;
    }

    @Override
    public final int hashCode() {
        return this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass().hashCode() : getClass().hashCode();
    }
}
