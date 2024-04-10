package io.goobi.vocabularyserver.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "language")
@Getter
@Setter
public class LanguageEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private long id;

    @Column(name = "abbreviation", nullable = false, unique = true)
    private String abbreviation;

    // `name` is a reserved Mysql keyword
    @Column(name = "full_name")
    private String name;
}
