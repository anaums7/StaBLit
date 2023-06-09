module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView = [hsx|
         <div style="background-color: red; padding: 2rem; color:hsla(196, 50%, 96%, 1); border-radius: 4px">
              <div style="max-width: 800px; margin-left: auto; margin-right: auto">
                  <h1 style="margin-bottom: 2rem; font-size: 2rem; font-weight: 300; border-bottom: 1px solid white; padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1)">
                      IHP
                  </h1>

                  <h2 style="margin-top: 0; margin-bottom: 0rem; font-weight: 900; font-size: 3rem">
                      Reddit
                  </h2>

                  <p style="margin-top: 1rem; font-size: 1.75rem; font-weight: 600; color:hsla(196, 13%, 80%, 1)">
                     Your new application is up and running.
                  </p>
                  
                  <a href="https://anaums7-urban-space-system-56pvjv6q4rjfpr5-8000.preview.app.github.dev/Posts" style="margin-top: 2rem; background-color: white; padding: 1rem; border-radius: 3px; color: red; text-decoration: none; font-weight: bold; display: inline-block; box-shadow: 0 4px 6px hsla(205, 69%, 0%, 0.08);  transition: box-shadow 0.2s; transition: transform 0.2s;" target="_blank">
                     Create Posts
                  </a>
              </div>
         </div>

         <div style="max-width: 800px; margin-left: auto; margin-right: auto; margin-top: 4rem">
              <img src="/ihp-welcome-icon.svg" alt="/ihp-welcome-icon" style="width:100%;">
              <p style="color: hsla(196, 13%, 50%, 1); margin-top: 4rem">
              </p>
         </div> 
|]