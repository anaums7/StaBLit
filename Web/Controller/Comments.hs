module Web.Controller.Comments where

import Web.Controller.Prelude
import Web.View.Comments.Index
import Web.View.Comments.New
import Web.View.Comments.Edit
import Web.View.Comments.Show

instance Controller CommentsController where
    action CommentsAction = do
        (commentsQ, pagination) <- query @Comment |> paginate
        comments <- commentsQ |> fetch
        render IndexView { .. }

    action NewCommentAction { postId }= do
        let comment = newRecord
                    |> set #postId postId
        post <- fetch postId
        render NewView { .. }

    action ShowCommentAction { commentId } = do
        comment <- fetch commentId
        render ShowView { .. }

    action EditCommentAction { commentId } = do
        comment <- fetch commentId
        render EditView { .. }

    action UpdateCommentAction { commentId } = do
        comment <- fetch commentId
        comment
            |> buildComment
            |> ifValid \case
                Left comment -> render EditView { .. }
                Right comment -> do
                    comment <- comment |> updateRecord
                    setSuccessMessage "Comment updated"
                    redirectTo (ShowPostAction ( get #postId comment ))

    action CreateCommentAction = do
        let comment = newRecord @Comment
        comment
            |> buildComment
            |> ifValid \case
                Left comment -> do 
                    post <- fetch comment.postId
                    render NewView { .. }
                Right comment -> do
                    comment <- comment |> createRecord
                    let postId = comment.postId
                    setSuccessMessage "Comment created"
                    redirectTo ShowPostAction { .. }

    action DeleteCommentAction { commentId } = do
        comment <- fetch commentId
        deleteRecord comment
        setSuccessMessage "Comment deleted"
        redirectTo (ShowPostAction ( get #postId comment ))

buildComment comment = comment
    |> fill @["postId", "author", "body"]
