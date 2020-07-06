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
            runStage('compile') {
                withGithubPrivkey {
                    sh 'make wc_compile'
                }
            }
   parallel lint: { 
            sh 'make wc_lint'
   }, xref: {
            sh 'make wc_xref'
        }, dialyze: {
            withDialyzerCache() {
                sh 'make wc_dialyze'
            }
        }, test: {
                sh "make wdeps_test"
        }
} 
