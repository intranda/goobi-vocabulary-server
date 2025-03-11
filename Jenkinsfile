def latestTag = ''
pipeline {
  agent none

  options {
    buildDiscarder logRotator(artifactDaysToKeepStr: '', artifactNumToKeepStr: '15', daysToKeepStr: '90', numToKeepStr: '')
    disableConcurrentBuilds()
  }

  stages {
    stage('build-snapshot') {
      agent {
        docker {
          image 'maven:3-eclipse-temurin-21'
          args '-v $HOME/.m2:/var/maven/.m2:z -v $HOME/.config:/var/maven/.config -v $HOME/.sonar:/var/maven/.sonar -u 1000 -ti -e _JAVA_OPTIONS=-Duser.home=/var/maven -e MAVEN_CONFIG=/var/maven/.m2'
        }
      }
      when {
        not {
          anyOf {
            branch 'master'
            branch 'release_*'
            branch 'hotfix_release_*'
            allOf {
              branch 'PR-*'
              expression { env.CHANGE_BRANCH.startsWith("release_") }
            }
          }
        }
      }
      steps {
        sh 'git reset --hard HEAD && git clean -fdx'
        sh 'mvn clean verify -U -P snapshot-build'
        junit "**/target/surefire-reports/*.xml"
        step([
          $class           : 'JacocoPublisher',
          execPattern      : '**/target/jacoco.exec',
          classPattern     : '**/target/classes/',
          sourcePattern    : 'src/main/java',
          exclusionPattern : '**/*Test.class'
        ])
        recordIssues (
          enabledForFailure: true, aggregatingResults: false,
          tools: [checkStyle(pattern: '**/target/checkstyle-result.xml', reportEncoding: 'UTF-8')]
        )
        archiveArtifacts artifacts: 'module-*/target/*.jar, install/*, module-core/src/main/resources/application.properties, migration/**', fingerprint: true
        stash includes: '**/target/*', name: 'target'
      }
    }
    stage('build-release') {
      agent {
        docker {
          image 'maven:3-eclipse-temurin-21'
          args '-v $HOME/.m2:/var/maven/.m2:z -v $HOME/.config:/var/maven/.config -v $HOME/.sonar:/var/maven/.sonar -u 1000 -ti -e _JAVA_OPTIONS=-Duser.home=/var/maven -e MAVEN_CONFIG=/var/maven/.m2'
        }
      }
      when {
        anyOf {
          branch 'master'
          branch 'release_*'
          branch 'hotfix_release_*'
          allOf {
            branch 'PR-*'
            expression { env.CHANGE_BRANCH.startsWith("release_") }
          }
        }
      }
      steps {
        sh 'git reset --hard HEAD && git clean -fdx'
        sh 'mvn clean verify -U -P release-build'
        junit "**/target/surefire-reports/*.xml"
        step([
          $class           : 'JacocoPublisher',
          execPattern      : '**/target/jacoco.exec',
          classPattern     : '**/target/classes/',
          sourcePattern    : 'src/main/java',
          exclusionPattern : '**/*Test.class'
        ])
        recordIssues (
          enabledForFailure: true, aggregatingResults: false,
          tools: [checkStyle(pattern: '**/target/checkstyle-result.xml', reportEncoding: 'UTF-8')]
        )
        archiveArtifacts artifacts: 'module-*/target/*.jar, install/*, module-core/src/main/resources/application.properties, migration/**', fingerprint: true
        stash includes: '**/target/*', name: 'target'
      }
    }
    /*stage('sonarcloud') {
      when {
        anyOf {
          branch 'master'
          branch 'release_*'
          branch 'hotfix_release_*'
          branch 'sonar_*'
          allOf {
            branch 'PR-*'
            expression { env.CHANGE_BRANCH.startsWith("release_") }
          }
        }
      }
      steps {
        withCredentials([string(credentialsId: 'jenkins-sonarcloud', variable: 'TOKEN')]) {
          sh 'mvn verify sonar:sonar -Dsonar.token=$TOKEN -U'
        }
      }
    }*/
    stage('deploy') {
      agent {
        docker {
          image 'maven:3-eclipse-temurin-21'
          args '-v $HOME/.m2:/var/maven/.m2:z -v $HOME/.config:/var/maven/.config -v $HOME/.sonar:/var/maven/.sonar -u 1000 -ti -e _JAVA_OPTIONS=-Duser.home=/var/maven -e MAVEN_CONFIG=/var/maven/.m2'
        }
      }
      when {
        anyOf {
        branch 'master'
        branch 'develop'
        branch 'hotfix_release_*'
        }
      }
      steps {
        unstash 'target'
        sh 'mvn -f module-exchange/pom.xml deploy -U'
      }
    }
    stage('tag release') {
      agent {
        docker {
          image 'maven:3-eclipse-temurin-21'
          args '-v $HOME/.m2:/var/maven/.m2:z -v $HOME/.config:/var/maven/.config -v $HOME/.sonar:/var/maven/.sonar -u 1000 -ti -e _JAVA_OPTIONS=-Duser.home=/var/maven -e MAVEN_CONFIG=/var/maven/.m2'
        }
      }
      when {
        anyOf {
          branch 'master'
          branch 'hotfix_release_*'
        }
      }
      steps {
        unstash 'target'
        withCredentials([gitUsernamePassword(credentialsId: '93f7e7d3-8f74-4744-a785-518fc4d55314',
                 gitToolName: 'git-tool')]) {
          sh '''#!/bin/bash -xe
              projectversion=$(mvn org.apache.maven.plugins:maven-help-plugin:3.4.0:evaluate -Dexpression=project.version -q -DforceStdout)
              if [ $? != 0 ]
              then
                  exit 1
              elif [[ "${projectversion}" =~ "SNAPSHOT" ]]
              then
                  echo "This is a SNAPSHOT version"
                  exit 1
              fi
              echo "${projectversion}"
              git tag -a "v${projectversion}" -m "releasing v${projectversion}" && git push origin v"${projectversion}"
          '''
          script {
            latestTag = sh(returnStdout: true, script:'git describe --tags --abbrev=0').trim()
          }
        }
      }
    }
    stage('build and publish production image to GitHub container registry') {
      agent any
      when {
        anyOf {
          branch 'master'
          branch 'develop'
          branch 'hotfix_release_*'
        }
      }
      steps {
        unstash 'target'
        script {
          docker.withRegistry('https://ghcr.io','jenkins-github-container-registry') {
            dockerimage_public = docker.build("intranda/goobi-vocabulary-server:${env.BUILD_ID}_${env.GIT_COMMIT}")
            if (env.GIT_BRANCH == 'origin/master' || env.GIT_BRANCH == 'master') {
              dockerimage_public.push("latest")
            }
            if (env.GIT_BRANCH == 'origin/develop' || env.GIT_BRANCH == 'develop') {
              dockerimage_public.push("develop")
            }
            if (latestTag != '') {
              dockerimage_public.push(latestTag)
            }
          }
        }
      }
    }
  }
  post {
    changed {
      emailext(
        subject: '${DEFAULT_SUBJECT}',
        body: '${DEFAULT_CONTENT}',
        recipientProviders: [requestor(),culprits()],
        attachLog: true
      )
    }
  }
}
/* vim: set ts=2 sw=2 tw=120 et :*/
