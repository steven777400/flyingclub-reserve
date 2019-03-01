{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Database.Persist.Owner.Instances where

import           Database.Persist.Owner.Class
import           Database.Persist.Schema

-- I didn't want to make these orphans, but otherwise there is a cycle
-- between schema and owner.class

instance Owner Address where
    owner = addressUserId
    ownerField = AddressUserId

instance Owner Email where
    owner = emailUserId
    ownerField = EmailUserId

instance Owner Phone where
    owner = phoneUserId
    ownerField = PhoneUserId

instance Owner Reservation where
    owner = reservationUserId
    ownerField = ReservationUserId

instance Owner Currency where
    owner = currencyUserId
    ownerField = CurrencyUserId