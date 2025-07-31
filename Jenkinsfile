pipeline {
    agent any
    
    environment {
        FPM_VERSION = '0.8.2'
    }
    
    stages {
        stage('Setup') {
            steps {
                sh '''
                    apt-get update -qq && apt-get install -y -qq gfortran wget
                    wget https://github.com/fortran-lang/fpm/releases/download/v${FPM_VERSION}/fpm-${FPM_VERSION}-linux-x86_64
                    chmod +x fpm-${FPM_VERSION}-linux-x86_64
                    mv fpm-${FPM_VERSION}-linux-x86_64 /usr/local/bin/fpm
                '''
            }
        }
        
        stage('Build') {
            steps {
                sh 'fpm build'
            }
        }
        
        stage('Test') {
            steps {
                sh 'fpm test'
            }
            post {
                always {
                    publishTestResults testResultsPattern: 'test-results.xml'
                }
            }
        }
        
        stage('Lint') {
            steps {
                sh './build/gfortran_*/app/fluff check src/ --format=github'
            }
            post {
                always {
                    archiveArtifacts artifacts: 'fluff-results.json', allowEmptyArchive: true
                }
            }
        }
    }
    
    post {
        always {
            cleanWs()
        }
    }
}