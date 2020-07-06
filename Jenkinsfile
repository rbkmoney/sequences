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
    runStage('check') {
        stages = [
            runStage('lint') {
                    sh 'make wc_lint'
            },
            runStage('xref') {
                    sh 'make wc_xref'
            },
            runStage('dialyze') {
                    withDialyzerCache() {
                        sh 'make wc_dialyze'
                }
            },
            runStage('test') {
                        sh "make wdeps_test"
            }
       parallel checks    
    }
} 

