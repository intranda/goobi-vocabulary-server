package io.goobi.vocabularyserver.model;

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
import lombok.Getter;
import lombok.Setter;
import org.hibernate.proxy.HibernateProxy;

import java.util.LinkedList;
import java.util.List;

@Entity
@Table(name = "vocabulary_record")
@Getter
@Setter
// Naming this class `Record` led to wrong behavior because of the introduction of Java records and some Spring Boot JPA logic
public class VocabularyRecordEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private long id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "vocabulary_id", nullable = false)
    private VocabularyEntity vocabulary;

    @OneToMany(mappedBy = "vocabularyRecord", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<FieldInstanceEntity> fields = new LinkedList<>();

    @ManyToOne
    @JoinColumn(name = "parent_record_id")
    private VocabularyRecordEntity parentRecord;

    @OneToMany(mappedBy = "parentRecord", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<VocabularyRecordEntity> children = new LinkedList<>();

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
        VocabularyRecordEntity vocabularyRecord = (VocabularyRecordEntity) o;
        return id == vocabularyRecord.id;
    }

    @Override
    public final int hashCode() {
        return this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass().hashCode() : getClass().hashCode();
    }
}
