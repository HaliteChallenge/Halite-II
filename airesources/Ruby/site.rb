class Site

  attr_reader :owner, :strength, :production

  def initialize(owner = 0, strength = 0, production = 0)
    @owner, @strength, @production = owner, strength, production
  end

end
