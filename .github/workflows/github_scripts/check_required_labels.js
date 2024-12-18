module.exports = async ({github, context, core}) => {
    const comments = await github.rest.issues.listComments({
        issue_number: context.issue.number,
        owner: context.repo.owner,
        repo: context.repo.repo
    });
    for (const comment of comments.data) {
        if (comment.body.includes('This pull request does not contain a valid label')) {
            github.rest.issues.deleteComment({
                issue_number: context.issue.number,
                owner: context.repo.owner,
                repo: context.repo.repo,
                comment_id: comment.id
            })
        }
    }
    github.rest.issues.createComment({
        issue_number: context.issue.number,
        owner: context.repo.owner,
        repo: context.repo.repo,
        body: 'This pull request does not contain a valid label. Please add one of the following labels: `[major, minor, patch, patch, skip]`'
    })
    core.setFailed('Missing required labels')
}
