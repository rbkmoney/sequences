#!groovy
// -*- mode: groovy -*-

def finalHook = {
  runStage('store CT logs') {
    archive '_build/test/logs/'
  }
}

build('sequences', 'docker-host', finalHook) {
  checkoutRepo()
  loadBuildUtils()

  def pipeErlangService
  runStage('load pipeline') {
    env.JENKINS_LIB = "build_utils/jenkins_lib"
    pipeErlangService = load("${env.JENKINS_LIB}/pipeErlangService.groovy")
  }

  //pipeErlangService.runPipe(false,true)

    def withDialyzerCache = load("${env.JENKINS_LIB}/withDialyzerCache.groovy")

              runStage('compile') {
                withGithubPrivkey {
                    sh 'make wc_compile'
                }
            }
    stage('Parallel') {
        failFast true
        parallel {
            stage('lint') {
                steps {
                    sh 'make wc_lint'
                }
            }
            stage('xref') {
                steps {
                    sh 'make wc_xref'
                }
            }
            stage('dialyze') {
                steps {
                    withDialyzerCache() {
                        sh 'make wc_dialyze'
                    }
                }
            }
            stage('test') {
                steps {
                        sh "make wdeps_test"
                }
            }
        }
    }
 
}
