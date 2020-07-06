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

  //pipeErlangService.runPipe(false)

            runStage('compile') {
                withGithubPrivkey {
                    sh 'make wc_compile'
                }
            }
        parallel test: {
            runStage('lint') {
        sh 'make wc_lint'
    }
    runStage('xref') {
        sh 'make wc_xref'
    }
}
} 
