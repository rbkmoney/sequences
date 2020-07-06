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
    runStage('Parallel') {
        parallel {
            runStage('lint') {
                steps {
                    sh 'make wc_lint'
                }
            }
            runStage('xref') {
                steps {
                    sh 'make wc_xref'
                }
            }
            runStage('dialyze') {
                steps {
                    withDialyzerCache() {
                        sh 'make wc_dialyze'
                    }
                }
            }
            runStage('test') {
                steps {
                        sh "make wdeps_test"
                }
            }
        }
    }
 
}
