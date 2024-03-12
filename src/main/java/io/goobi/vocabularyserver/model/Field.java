package io.goobi.vocabularyserver.model;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.lang.NonNull;

@Entity
@Table(name = "VocabularyField")
@Data
@RequiredArgsConstructor
@NoArgsConstructor
public class Field {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Integer id;

    @NonNull
    private String name;
    @NonNull
    private String value;
}
